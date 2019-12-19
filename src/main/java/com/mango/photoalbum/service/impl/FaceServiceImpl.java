package com.mango.photoalbum.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.mango.photoalbum.config.ThreadPoolHelper;
import com.mango.photoalbum.constant.FaceConstant;
import com.mango.photoalbum.constant.QueueConstant;
import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.FaceInfo;
import com.mango.photoalbum.model.FaceInfoCo;
import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.model.UploadFileFace;
import com.mango.photoalbum.service.FaceService;
import com.mango.photoalbum.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.awt.image.BufferedImage;
import java.util.Date;
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

    @Resource
    private OssUtils oss;

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
                    List<UploadFileFace> uploadFileFaces = face.recognizeFace(FaceInfo.builder()
                            .imageUrl(file.getFilePath())
                            .group(FaceConstant.GROUP_DEFAUlT)
                            .build());
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

    @Override
    public void save(FaceInfoCo faceInfoCo) {
        MultipartFile file = faceInfoCo.getFile();
        if(!CommonUtils.isNullOrEmpty(file)) {
            try {
                UploadFile uploadFile = UploadFile.builder()
                        .fileId(CommonUtils.UUID())
                        .createTime(new Date())
                        .modifyTime(new Date())
                        .albumId("")
                        .remark("")
                        .createUserId(faceInfoCo.getUserId())
                        .modifyUserId(faceInfoCo.getUserId())
                        .isDel(IsDelEnum.FALSE.getValue())
                        .persons(faceInfoCo.getUserId().toString())
                        .build();
                String fileName = file.getOriginalFilename();
                String fileType = FileUtils.getFileType(fileName);
                uploadFile.setFileType(fileType);
                uploadFile.setFileSize((int) file.getSize());
                uploadFile.setFileName(fileName);
                BufferedImage img = FileUtils.toImage(file);
                uploadFile.setHeight(img.getHeight());
                uploadFile.setWidth(img.getWidth());
                String ossFileName = uploadFile.getFileId() + fileType;
                //oss上传图片
                oss.save(file.getInputStream(), ossFileName);
                //oss文件路径获取
                uploadFile.setFilePath(oss.getViewUrl(ossFileName));
                //ots保存文件信息
                ots.creatRow(uploadFile);
                log.info("保存文件信息成功:{}", uploadFile.toString());
                FaceInfo faceInfo = FaceInfo.builder()
                        .faceId(CommonUtils.UUID())
                        .group(FaceConstant.GROUP_DEFAUlT)
                        .imageUrl(uploadFile.getFilePath())
                        .image(uploadFile.getCreateUserId().toString())
                        .person(uploadFile.getCreateUserId().toString())
                        .content("")
                        .fileId(uploadFile.getFileId())
                        .createTime(new Date())
                        .build();
                //face图片注册
                face.addFace(faceInfo);
                //ots保存人脸信息
                ots.creatRow(faceInfo);
                log.info("保存人脸信息成功:{}", faceInfo.toString());
            } catch (Exception e) {
                e.printStackTrace();
                log.error("保存人脸信息出错:{}", e.getMessage());
                throw new RuntimeException(e);
            }
        } else {
            throw new RuntimeException("图片不可以为空");
        }
    }
}
