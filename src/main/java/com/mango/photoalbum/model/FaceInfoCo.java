package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;

@Data
@ApiModel("人脸信息")
public class FaceInfoCo {

    @ApiModelProperty(value = "文件", required = true)
    private MultipartFile file;

    @ApiModelProperty(value = "上传人id", required = true)
    private Integer userId;
}
