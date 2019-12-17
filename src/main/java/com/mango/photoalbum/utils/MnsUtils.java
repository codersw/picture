package com.mango.photoalbum.utils;

import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.aliyun.mns.client.CloudAccount;
import com.aliyun.mns.client.CloudQueue;
import com.aliyun.mns.client.MNSClient;
import com.aliyun.mns.model.Message;
import com.aliyun.mns.model.QueueMeta;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import javax.annotation.PostConstruct;

/**
 * mns工具类
 * @author swen
 */
@Slf4j
@Component
public class MnsUtils {

    @Value("${alibaba.mns.accessKeyId}")
    private String accessKeyId;

    @Value("${alibaba.mns.accessKeySecret}")
    private String accessKeySecret;

    @Value("${alibaba.mns.endpoint}")
    private String endpoint;

    private MNSClient mns;

    @PostConstruct
    private void init() {
        try {
            CloudAccount account = new CloudAccount(accessKeyId, accessKeySecret, endpoint);
            mns = account.getMNSClient();
        } catch (Exception e){
            e.printStackTrace();
            mns = null;
            log.error("初始化mns出错:{}", e.getMessage());
        }
    }

    /**
     * 创建队列
     * @param queueName
     */
    public void createQueue(String queueName) {
        try {
            log.info("MNS创建队列开始:{}", queueName);
            QueueMeta meta = new QueueMeta(); //生成本地QueueMeta属性，有关队列属性详细介绍见https://help.aliyun.com/document_detail/27476.html
            meta.setQueueName(queueName);  // 设置队列名
            meta.setPollingWaitSeconds(15); //设置队列消息的长轮询等待时间，单位是秒
            meta.setMaxMessageSize(2048L); //设置队列消息的最大长度，单位是byte long
            CloudQueue queue = mns.createQueue(meta);
            log.info("MNS创建队列成功:{}", JSONObject.toJSONString(queue, SerializerFeature.IgnoreNonFieldGetter));
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS创建队列出现错误:{}", e.getMessage());
        }
    }

    /**
     * 删除队列
     * @param queueName
     */
    public void deleteQueue(String queueName) {
        try {
            log.info("MNS删除队列开始:{}", queueName);
            CloudQueue queue = mns.getQueueRef(queueName);
            queue.delete();
            log.info("MNS删除队列成功");
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS删除队列出现错误:{}", e.getMessage());
        }
    }

    /**
     * 接收消息
     * @param queueName
     * @return
     */
    public String getMessage(String queueName) {
        String result = "";
        try {
            log.info("MNS接收消息开始:{}", queueName);
            CloudQueue queue = mns.getQueueRef(queueName);
            Message popMsg = queue.popMessage();
            if (!CommonUtils.isNullOrEmpty(popMsg)) {
                result = popMsg.getMessageBodyAsString();
                //删除已经取出消费的消息
                queue.deleteMessage(popMsg.getReceiptHandle());
            }
            log.info("MNS接收消息成功:{}", JSONObject.toJSONString(popMsg, SerializerFeature.IgnoreNonFieldGetter));
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS创建队列出现错误:{}", e.getMessage());
        }
        return result;
    }

    /**
     * 发送消息
     * @param queueName
     * @param bodyStr
     */
    public void setMessage(String queueName, String bodyStr) {
        try {
            log.info("MNS发送消息开始:{}, {}", queueName, bodyStr);
            CloudQueue queue = mns.getQueueRef(queueName);
            Message message = new Message();
            message.setMessageBody(bodyStr);
            Message putMsg = queue.putMessage(message);
            log.info("MNS发送消息成功:{}", JSONObject.toJSONString(putMsg, SerializerFeature.IgnoreNonFieldGetter));
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS发送消息出现错误:{}", e.getMessage());
        }
    }

    /**
     * 删除消息
     * @param queueName
     * @param receiptHandle
     */
    public void deleteMessage(String queueName, String receiptHandle) {
        try {
            log.info("MNS删除消息开始:{}", receiptHandle);
            CloudQueue queue = mns.getQueueRef(queueName);
            queue.deleteMessage(receiptHandle);
            log.info("MNS删除消息成功");
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS删除消息出现错误:{}", e.getMessage());
        }
    }
}
