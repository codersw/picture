package com.mango.photoalbum.config;

import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.acl.common.AclClientRPCHook;
import org.apache.rocketmq.acl.common.SessionCredentials;
import org.apache.rocketmq.client.AccessChannel;
import org.apache.rocketmq.client.consumer.DefaultMQPushConsumer;
import org.apache.rocketmq.client.consumer.listener.ConsumeConcurrentlyStatus;
import org.apache.rocketmq.client.consumer.listener.MessageListenerConcurrently;
import org.apache.rocketmq.client.consumer.rebalance.AllocateMessageQueueAveragely;
import org.apache.rocketmq.client.producer.DefaultMQProducer;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.common.message.Message;
import org.apache.rocketmq.remoting.common.RemotingHelper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * RocketMQ配置注入
 * @author swen
 */
@Slf4j
@Configuration
public class RocketMQConfig {

    //鉴权accesskey
    @Value("${aliyun.mq.accesskey}")
    private String accesskey;
    //鉴权secretkey
    @Value("${aliyun.mq.secretkey}")
    private String secretkey;
    //接入点
    @Value("${aliyun.mq.namesrvAddr}")
    private String namesrvAddr;
    //主题
    @Value("${aliyun.mq.topic}")
    private String topic;
    //生产者id
    @Value("${aliyun.mq.producerId:DEFAULT_PRODUCER}")
    private String producerId;
    //消费者id
    @Value("${aliyun.mq.consumerId:DEFAULT_CONSUMER}")
    private String consumerId;
    //接收所有节点
    @Value("${aliyun.mq.subExpression:*}")
    private String subExpression;
    //tags可设置topic节点
    @Value("${aliyun.mq.tags:1}")
    private String tags;

    /**
     * 注入生产者
     * @return
     */
    @Bean
    public DefaultMQProducer producer() {
        try {
            //设置为云上创建的 GID, 以及替换为自己的 AccessKeyId 和 AccessKeySecret
            DefaultMQProducer producer = new DefaultMQProducer(producerId,
                    new AclClientRPCHook(new SessionCredentials(accesskey, secretkey)));
            //设置为自己的云上接入点
            producer.setNamesrvAddr(namesrvAddr);
            // 云上消息轨迹需要设置为 CLOUD
            producer.setAccessChannel(AccessChannel.CLOUD);
            // 设置为云上创建的 Topic 名字
            Message msg = new Message(topic, tags, "Hello RocketMQ".getBytes(RemotingHelper.DEFAULT_CHARSET));
            SendResult sendResult = producer.send(msg);
            log.info("RocketMQ发送消息成功: {}", sendResult);
            return producer;
        } catch (Exception e) {
            log.error("初始化RocketMQMQ生产者出错：{}", e.getMessage());
            return null;
        }
    }

    /**
     * 注入消费者
     * @return
     */
    @Bean
    public DefaultMQPushConsumer consumer() {
        try {
            //设置为云上创建的 GID, 以及替换为自己的 AccessKeyId 和 AccessKeySecret
            DefaultMQPushConsumer consumer = new DefaultMQPushConsumer(consumerId,
                    new AclClientRPCHook(new SessionCredentials(accesskey, secretkey)), new AllocateMessageQueueAveragely());
            //设置为云上接入点
            consumer.setNamesrvAddr(namesrvAddr);
            // 云上消息轨迹需要设置为 CLOUD
            consumer.setAccessChannel(AccessChannel.CLOUD);
            // 设置为云上创建的 Topic
            consumer.subscribe(topic, subExpression);
            consumer.registerMessageListener((MessageListenerConcurrently) (msg, context) -> {
                log.info("RocketMQ接收到消息: {}", msg);
                return ConsumeConcurrentlyStatus.CONSUME_SUCCESS;
            });
            consumer.start();
            return consumer;
        } catch (Exception e) {
            log.error("初始化RocketMQMQ消费者出错：{}", e.getMessage());
            return null;
        }
    }
}
