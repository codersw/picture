package com.mango.photoalbum.service.impl;

import com.mango.photoalbum.constant.FaceConstant;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.ConsumeMode;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Service;


@Slf4j
@Service
@RocketMQMessageListener(topic = FaceConstant.ROCKET_MQ_TOPIC_FACE, consumerGroup = FaceConstant.ROCKET_MQ_GROUP_FACE,
        consumeMode = ConsumeMode.ORDERLY, accessKey = "${aliyun.mq.accessKey}", secretKey = "${aliyun.mq.secretKey}")
public class FaceConsumer implements RocketMQListener {

    @Override
    public void onMessage(Object message) {
        log.info("监听到信息:{}", message.toString());
    }
}
