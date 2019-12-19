package com.mango.photoalbum.utils;

import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.aliyun.mns.client.CloudAccount;
import com.aliyun.mns.client.CloudQueue;
import com.aliyun.mns.client.CloudTopic;
import com.aliyun.mns.client.MNSClient;
import com.aliyun.mns.common.http.ClientConfiguration;
import com.aliyun.mns.model.*;
import com.aliyun.mns.sample.HttpEndpoint;
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

    @Value("${alibaba.mns.threadNum:100}")
    private int threadNum;

    @Value("${alibaba.mns.totalSeconds:180}")
    private int totalSeconds;

    private MNSClient mns;

    @PostConstruct
    private void init() {
        try {
            ClientConfiguration clientConfiguration = new ClientConfiguration();
            clientConfiguration.setMaxConnections(threadNum);
            clientConfiguration.setMaxConnectionsPerRoute(threadNum);
            CloudAccount account = new CloudAccount(accessKeyId, accessKeySecret, endpoint, clientConfiguration);
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
     * 创建topic
     * @param topicName
     */
    public void createTopic(String topicName) {
        try {
            log.info("MNS创建topic开始:{}", topicName);
            TopicMeta meta = new TopicMeta();
            meta.setTopicName(topicName);
            CloudTopic topic = mns.createTopic(meta);
            log.info("MNS创建topic成功：{}", JSONObject.toJSONString(topic, SerializerFeature.IgnoreNonFieldGetter));
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS创建topic出现错误:{}", e.getMessage());
        }
    }

    /**
     * 删除topic
     * @param topicName
     */
    public void deleteTopic(String topicName) {
        try {
            log.info("MNS删除topic开始:{}", topicName);
            CloudTopic topic = mns.getTopicRef(topicName);
            topic.delete();
            log.info("MNS删除topic成功：{}", JSONObject.toJSONString(topic, SerializerFeature.IgnoreNonFieldGetter));
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS删除topic出现错误:{}", e.getMessage());
        }
    }

    /**
     * 创建订阅
     * @param topicName
     * @param subscriptionName
     */
    public void subscribe(String topicName, String subscriptionName) {
        try {
            log.info("MNS创建订阅开始:{},{}", topicName, subscriptionName);
            CloudTopic topic = mns.getTopicRef(topicName);
            SubscriptionMeta subMeta = new SubscriptionMeta();
            subMeta.setSubscriptionName(subscriptionName);
            subMeta.setEndpoint(HttpEndpoint.GenEndpointLocal());
            subMeta.setNotifyContentFormat(SubscriptionMeta.NotifyContentFormat.XML);
            //subMeta.setFilterTag("filterTag"); //设置订阅的filterTag
            String subUrl = topic.subscribe(subMeta);
            log.info("MNS创建订阅成功:{}", subUrl);
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS创建订阅出现错误:{}", e.getMessage());
        }
    }

    /**
     * 发布消息
     * @param topicName
     * @param messageBody
     */
    public void publishMessage(String topicName, String messageBody) {
        try {
            log.info("MNS发布消息开始:{},{}", topicName, messageBody);
            CloudTopic topic = mns.getTopicRef(topicName);
            TopicMessage msg = new Base64TopicMessage(); //可以使用TopicMessage结构，选择不进行Base64加密
            msg.setMessageBody(messageBody);
            //msg.setMessageTag("filterTag"); //设置该条发布消息的filterTag
            msg = topic.publishMessage(msg);
            log.info("MNS创建订阅成功:{}", JSONObject.toJSONString(msg, SerializerFeature.IgnoreNonFieldGetter));
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS创建订阅出现错误:{}", e.getMessage());
        }
    }

    /**
     * 取消订阅
     * @param topicName
     * @param subscriptionName
     */
    public void unSubscribe(String topicName, String subscriptionName) {
        try {
            log.info("MNS取消订阅开始:{},{}", topicName, subscriptionName);
            CloudTopic topic = mns.getTopicRef(topicName);
            topic.unsubscribe(subscriptionName);
            log.info("MNS取消订阅成功");
        } catch (Exception e) {
            e.printStackTrace();
            log.info("MNS取消订阅出现错误:{}", e.getMessage());
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
