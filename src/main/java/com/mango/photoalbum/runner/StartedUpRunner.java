package com.mango.photoalbum.runner;

import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.stereotype.Component;
import javax.annotation.Resource;
import java.time.LocalDateTime;

@Slf4j
@Component
public class StartedUpRunner implements ApplicationRunner {

    @Resource
    private ConfigurableApplicationContext context;

    @Value("${spring.application.name:photoalbum}")
    private String applicationName;

    @Resource
    private OtsUtils ots;

    @Override
    public void run(ApplicationArguments args) {
        if (context.isActive()) {
           /* //相册
            ots.creatTable(PhotoAlbum.class);
            ots.deleteSearchIndex(PhotoAlbum.class);
            ots.createSearchIndex(PhotoAlbum.class);
            //文件
            ots.creatTable(UploadFile.class);
            ots.deleteSearchIndex(UploadFile.class);
            ots.createSearchIndex(UploadFile.class);
            //文件识别
            ots.creatTable(UploadFileFace.class);
            ots.deleteSearchIndex(UploadFileFace.class);
            ots.createSearchIndex(UploadFileFace.class);
            //人脸信息
            ots.creatTable(FaceInfo.class);
            ots.deleteSearchIndex(FaceInfo.class);
            ots.createSearchIndex(FaceInfo.class);*/
            log.info("  _   _   _   _   _   _   _   _");
            log.info(" / \\ / \\ / \\ / \\ / \\ / \\ / \\ / \\");
            log.info("( c | o | m | p | l | e | t | e )");
            log.info(" \\_/ \\_/ \\_/ \\_/ \\_/ \\_/ \\_/ \\_/");
            log.info("{} 启动完毕，时间：{}", applicationName, LocalDateTime.now());
        }
    }
}
