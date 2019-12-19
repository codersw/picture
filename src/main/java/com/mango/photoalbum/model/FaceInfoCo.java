package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;

@Data
@ApiModel("相册")
public class FaceInfoCo {

    @ApiModelProperty(value = "人脸的姓名", required = true)
    private String person;

    @ApiModelProperty(value = "人脸的编号", required = true)
    private String image;

    @ApiModelProperty(value = "文件")
    private MultipartFile file;

    @ApiModelProperty(value = "上传人id", required = true)
    private Integer userId;
}
