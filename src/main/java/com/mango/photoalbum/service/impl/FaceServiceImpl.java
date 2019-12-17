package com.mango.photoalbum.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.mango.photoalbum.config.ThreadPoolHelper;
import com.mango.photoalbum.constant.FaceConstant;
import com.mango.photoalbum.constant.QueueConstant;
import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.model.UploadFileFace;
import com.mango.photoalbum.service.FaceService;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.FaceUtils;
import com.mango.photoalbum.utils.MnsUtils;
import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class FaceServiceImpl implements FaceService {

    @Resource
    private FaceUtils face;

    @Resource
    private MnsUtils mns;

    @Resource
    private OtsUtils ots;

    @PostConstruct
    private void init() {
        ThreadPoolHelper pool = new ThreadPoolHelper();
        pool.Executor(this::getMessage);
    }

    /**
     * 获取消息
     */
    private void getMessage() {
        while (true) {
            try {
                String bodyStr = mns.getMessage(QueueConstant.FACEQUEUE);
                if(!StringUtils.isEmpty(bodyStr)) {
                    UploadFile file = JSONObject.parseObject(bodyStr, UploadFile.class);
                    List<UploadFileFace> uploadFileFaces = face.recognizeFace(FaceConstant.DEFAUlT, file.getFilePath());
                    if(!CommonUtils.isNullOrEmpty(uploadFileFaces)) {
                        uploadFileFaces.forEach(uploadFileFace -> {
                            uploadFileFace.setFileId(file.getFileId());
                            ots.creatRow(uploadFileFace);
                        });
                        List<String> persons = uploadFileFaces.stream().filter(x -> x.getScore() >= FaceConstant.SCORE).map(UploadFileFace::getPerson).collect(Collectors.toList());
                        ots.updataRow(UploadFile.builder()
                                .fileId(file.getFileId())
                                .persons(String.join(",", persons))
                                .build());
                    }
                }
                Thread.sleep(1000);
            } catch (Exception e) {
                e.printStackTrace();
                log.error("MNS获取消息发生错误:{}" , e.getMessage());
            }
        }
    }
}
