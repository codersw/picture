package com.mango.photoalbum.controller;

import com.mango.photoalbum.model.UploadFileCo;
import com.mango.photoalbum.model.UploadFileMultiCo;
import com.mango.photoalbum.model.UploadFile;
import com.mango.photoalbum.model.Result;
import com.mango.photoalbum.model.ResultGenerator;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.FileUtils;
import com.mango.photoalbum.utils.OssUtils;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * 文件接口
 * @author swen
 */
@Api(value = "文件接口", tags = {"文件接口"})
@Slf4j
@RestController
@RequestMapping("/upload")
public class UploadFileController {

    @Autowired
    private OssUtils oss;

    /**
     * 上传图片
     * @param uploadFileCo
     * @return
     */
    @ApiOperation(value = "文件上传接口", notes = "文件上传接口")
    @ApiImplicitParams({@ApiImplicitParam(paramType = "form", dataType="__file", name = "file",
            value = "文件", required = true)})
    @PostMapping(value = "/file", headers = "content-type=multipart/form-data")
    public Result file(@ModelAttribute UploadFileCo uploadFileCo) {
        try {
            if(!CommonUtils.isNullOrEmpty(uploadFileCo.getFile())){
                if(!Objects.requireNonNull(uploadFileCo.getFile().getContentType()).contains("image")){
                    return ResultGenerator.genFailResult("上传文件类型只可以是图片");
                }
                String fileType = FileUtils.getFileType(uploadFileCo.getFile().getOriginalFilename());
                if(StringUtils.isBlank(uploadFileCo.getFileId())){
                    uploadFileCo.setFileId(CommonUtils.UUID());
                }
                String ossFileName = uploadFileCo.getFileId() + fileType;
                oss.save(uploadFileCo.getFile().getInputStream(), ossFileName );
                UploadFile uploadFile = UploadFile.builder()
                        .fileId(uploadFileCo.getFileId())
                        .fileName(uploadFileCo.getFile().getOriginalFilename())
                        .filePath(oss.getViewUrl(ossFileName))
                        .fileSize(uploadFileCo.getFile().getSize())
                        .fileType(fileType)
                        .createTime(new Date())
                        .modifyTime(new Date())
                        .remark(uploadFileCo.getRemark())
                        .userId(uploadFileCo.getUserId())
                        .build();
                return ResultGenerator.genSuccessResult(uploadFile);
            }else{
                return ResultGenerator.genFailResult("图片不可以是空的");
            }
        }catch (Exception e){
            e.printStackTrace();
            log.error("上传图片发生异常{}", e.getMessage());
            return ResultGenerator.genFailResult("上传图片发生异常");
        }
    }


    /**
     * 上传图片
     * @param uploadFileMultiCo
     * @return
     */
    @ApiOperation(value = "文件批量上传接口", notes = "swagger批量文件上传不好用请用postman等工具测试")
    @PostMapping(value = "/files", headers = "content-type=multipart/form-data")
    public Result files(@ModelAttribute UploadFileMultiCo uploadFileMultiCo) {
        try {
            List<UploadFile> result = new ArrayList<>();
            List<UploadFileCo> uploadFileCos = uploadFileMultiCo.getUploadFileCos();
            uploadFileCos.forEach(uploadFileCo ->{
                if(Objects.requireNonNull(uploadFileCo.getFile().getContentType()).contains("image")){
                    String fileType = FileUtils.getFileType(uploadFileCo.getFile().getOriginalFilename());
                    if(StringUtils.isBlank(uploadFileCo.getFileId())){
                        uploadFileCo.setFileId(CommonUtils.UUID());
                    }
                    String ossFileName = uploadFileCo.getFileId() + fileType;
                    try {
                        oss.save(uploadFileCo.getFile().getInputStream(), ossFileName );
                    } catch (IOException e) {
                        e.printStackTrace();
                        log.error("上传图片发生异常{}", e.getMessage());
                    }
                    result.add(UploadFile.builder()
                            .fileId(uploadFileCo.getFileId())
                            .fileName(uploadFileCo.getFile().getOriginalFilename())
                            .filePath(oss.getViewUrl(ossFileName))
                            .fileSize(uploadFileCo.getFile().getSize())
                            .fileType(fileType)
                            .createTime(new Date())
                            .modifyTime(new Date())
                            .remark(uploadFileCo.getRemark())
                            .userId(uploadFileCo.getUserId())
                            .build());
                }
            });
            return ResultGenerator.genSuccessResult(result);
        }catch (Exception e){
            e.printStackTrace();
            log.error("上传图片发生异常{}", e.getMessage());
            return ResultGenerator.genFailResult("上传图片发生异常");
        }
    }
}
