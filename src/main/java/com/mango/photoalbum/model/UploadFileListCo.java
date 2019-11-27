package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import javax.validation.constraints.NotBlank;

@Data
@ApiModel("文件列表")
public class UploadFileListCo {

    @ApiModelProperty(value = "相册id", required = true)
    @NotBlank(message = "相册id不可以为空")
    private String albumId;

    /**
     * 当前页
     */
    @ApiModelProperty(value = "当前页")
    private Integer pageIndex = 0;

    /**
     * 每页总条数
     */
    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = 0;
}
