package com.mango.picture.utils;

import com.aliyun.mns.client.CloudAccount;
import com.aliyun.mns.client.CloudQueue;
import com.aliyun.mns.client.MNSClient;
import com.aliyun.mns.common.ClientException;
import com.aliyun.mns.common.ServiceException;
import com.aliyun.mns.model.Message;
import com.mango.picture.config.ThreadPoolHelper;
import com.mango.picture.constant.QueueConstant;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

@Slf4j
@Component
@Data
public class MnsUtils {

    private static String accessKeyId;

    private static String accessKeySecret;

    private static String endpoint;

    private static MNSClient client;

    @Value("${alibaba.mns.accessKeyId}")
    public void setAccessKeyId(String accessKeyId) {
        MnsUtils.accessKeyId = accessKeyId;
    }

    @Value("${alibaba.mns.accessKeySecret}")
    public void setAccessKeySecret(String accessKeySecret) {
        MnsUtils.accessKeySecret = accessKeySecret;
    }

    @Value("${alibaba.mns.endpoint}")
    public void setEndpoint(String endpoint) {
        MnsUtils.endpoint = endpoint;
    }

    static {
        try {
            CloudAccount account = new CloudAccount(accessKeyId, accessKeySecret, endpoint);
            client = account.getMNSClient();
        } catch (Exception e){
          e.printStackTrace();
          client = null;
          log.error("创建client失败:{}", e.getMessage());
        }
    }

    //@PostConstruct
    public void Work() {
        ThreadPoolHelper pool = new ThreadPoolHelper();
        pool.Executor(this::ThreadWork);
    }

    /**
     * 消费服务
     */
    private void ThreadWork() {
        log.info("MNS消息服务消费端启动成功");
        while (true) {
            log.info("MNS消息服务开始消费");
            try{
                CloudQueue queue = client.getQueueRef(QueueConstant.FACEQUEUE);
                Message popMsg = queue.popMessage();
                if (popMsg != null) {
                    // 默认会做 base64 解码
                    String bodyStr = popMsg.getMessageBodyAsString();
                    String msgId = popMsg.getMessageId();
                    log.info("message id:{},message body:{}", msgId, bodyStr);
                    // TODO:进行消费处理，人脸识别，成功则删除消息

                    //删除已经消费的消息
                    queue.deleteMessage(popMsg.getReceiptHandle());
                    log.info("删除MNS消息服务成功");
                } else {
                    log.info("MNS消息服务不存在");
                }
                log.info("MNS消息服务消费完成");
            } catch (ClientException ce) {
                log.error("MMS链接异常:{}", ce.getMessage());
                ce.printStackTrace();
            }  catch (ServiceException se) {
                se.printStackTrace();
                log.error("MNS业务异常:{},requestId:{}", se.getMessage(), se.getRequestId());
                if (se.getErrorCode() != null) {
                    if (se.getErrorCode().equals("QueueNotExist")) {
                        log.error("MNS队列不存在:{}", se.getMessage());
                    } else if (se.getErrorCode().equals("TimeExpired")) {
                        log.error("MNS认证已过期:{}", se.getMessage());
                    }
                }
            } catch (Exception e) {
                log.error("MNS发生异常:{}", e.getMessage());
                e.printStackTrace();
            }
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                log.error("MNS暂停发生异常:{}", e.getMessage());
                e.printStackTrace();
            }
        }
    }
}
