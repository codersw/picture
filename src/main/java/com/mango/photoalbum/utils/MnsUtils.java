package com.mango.photoalbum.utils;

import com.aliyun.mns.client.CloudAccount;
import com.aliyun.mns.client.CloudQueue;
import com.aliyun.mns.client.MNSClient;
import com.aliyun.mns.common.ClientException;
import com.aliyun.mns.common.ServiceException;
import com.aliyun.mns.model.Message;
import com.mango.photoalbum.config.ThreadPoolHelper;
import com.mango.photoalbum.constant.QueueConstant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class MnsUtils {

    @Value("${alibaba.mns.accessKeyId}")
    private String accessKeyId;

    @Value("${alibaba.mns.accessKeySecret}")
    private String accessKeySecret;

    @Value("${alibaba.mns.endpoint}")
    private String endpoint;

    private MNSClient client;

    //@PostConstruct
    private void init() {
        try {
            CloudAccount account = new CloudAccount(accessKeyId, accessKeySecret, endpoint);
            client = account.getMNSClient();
            ThreadPoolHelper pool = new ThreadPoolHelper();
            pool.Executor(this::ThreadWork);
        } catch (Exception e){
            e.printStackTrace();
            client = null;
            log.error("创建mns client失败:{}", e.getMessage());
        }
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
