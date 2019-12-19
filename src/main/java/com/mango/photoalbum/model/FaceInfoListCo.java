package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
@ApiModel("人脸信息")
public class FaceInfoListCo {

    @ApiModelProperty(value = "用户id")
    private Integer userId;

    @ApiModelProperty(value = "文件id")
    private String fileId;

    @ApiModelProperty(value = "当前页")
    private Integer pageIndex = 1;

    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = 20;
}
