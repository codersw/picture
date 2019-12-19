package com.mango.photoalbum.model;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

@Data
@ApiModel("文件识别")
public class UploadFileFaceListCo {

    @ApiModelProperty(value = "用户id")
    private Integer userId;


    @ApiModelProperty(value = "文件id")
    private String fileId;
}