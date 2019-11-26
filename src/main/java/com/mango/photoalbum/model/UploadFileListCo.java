package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotNull;

@Data
@ApiModel("文件列表")
public class UploadFileListCo {

    /**
     * 当前页
     */
    @ApiModelProperty(value = "当前页", required = true)
    @NotNull(message = "请传入当前页")
    private Integer pageIndex = 1;

    /**
     * 每页总条数
     */
    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = 20;
}
