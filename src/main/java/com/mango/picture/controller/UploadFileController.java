package com.mango.picture.controller;

import com.mango.picture.model.UploadFile;
import com.mango.picture.result.Result;
import com.mango.picture.result.ResultGenerator;
import com.mango.picture.utils.CommonUtils;
import com.mango.picture.utils.FileUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import java.util.Date;
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

    /**
     * 上传图片
     * @param file
     * @return
     */
    @ApiOperation(value = "文件上传接口", notes = "文件上传接口")
    @PostMapping(value = "/file", headers = "content-type=multipart/form-data")
    public Result file(@ApiParam(value = "文件", required = true) MultipartFile file) {
        try {
            if(!CommonUtils.isNullOrEmpty(file)){
                if(!Objects.requireNonNull(file.getContentType()).contains("image")){
                    return ResultGenerator.genFailResult("上传文件类型只可以是图片");
                }
                UploadFile uploadFile = UploadFile.builder()
                        .fileName(file.getOriginalFilename())
                        .filePath( "/upload/"+ FileUtils.uploadFile(file, ""))
                        .fileSize(file.getSize())
                        .fileType(FileUtils.getFileType(file.getOriginalFilename()))
                        .createDate(new Date())
                        .build();
                return ResultGenerator.genSuccessResult(uploadFile);
            }else{
                return ResultGenerator.genFailResult("图片不可以是空的");
            }
        }catch (Exception e){
            e.printStackTrace();
            log.error("上传图片发生异常{}", e.getMessage());
            return  ResultGenerator.genFailResult("上传图片发生异常");
        }
    }
}
