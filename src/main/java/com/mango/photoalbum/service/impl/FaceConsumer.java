package com.mango.photoalbum.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.client.consumer.DefaultMQPushConsumer;
import org.apache.rocketmq.spring.annotation.ConsumeMode;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.apache.rocketmq.spring.core.RocketMQPushConsumerLifecycleListener;
import org.springframework.stereotype.Service;


@Slf4j
@Service
@RocketMQMessageListener(topic = "${aliyun.mq.topic}", consumerGroup = "${aliyun.mq.consumerId:DEFAULT_CONSUMER}", consumeMode = ConsumeMode.ORDERLY)
public class FaceConsumer implements RocketMQListener, RocketMQPushConsumerLifecycleListener {


    @Override
    public void onMessage(Object o) {

    }

    @Override
    public void prepareStart(DefaultMQPushConsumer defaultMQPushConsumer) {
        //设置重试次数 无限重试-1
        defaultMQPushConsumer.setMaxReconsumeTimes(-1);
    }
}
