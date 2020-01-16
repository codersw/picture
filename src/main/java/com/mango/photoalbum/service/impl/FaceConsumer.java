package com.mango.photoalbum.service.impl;

import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.service.FaceService;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.ConsumeMode;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.concurrent.TimeUnit;

/**
 * topic消费
 * 默认出现异常无限重试
 * 如需修改实现 RocketMQPushConsumerLifecycleListener 重写
 * @author swen
 */
@Slf4j
@Service
@RocketMQMessageListener(topic = "${alibaba.mq.topic}", consumerGroup = "${alibaba.mq.group}",
        accessKey = "${alibaba.mq.accessKeyId}", secretKey = "${alibaba.mq.accessKeySecret}",
        consumeMode = ConsumeMode.ORDERLY, consumeThreadMax = 1)
public class FaceConsumer implements RocketMQListener<UploadFile> {

    @Resource
    private FaceService faceService;

    @SneakyThrows
    @Override
    public void onMessage(UploadFile uploadFile) {
        log.info("监听到信息:{}", uploadFile.toString());
        faceService.handleFace(uploadFile);
        TimeUnit.SECONDS.sleep(1);
    }
}
