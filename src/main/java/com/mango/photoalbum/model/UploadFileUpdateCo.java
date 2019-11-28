package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import javax.validation.constraints.NotBlank;

@Data
@ApiModel("修改文件")
public class UploadFileUpdateCo {

    @ApiModelProperty(value = "文件id", required = true)
    @NotBlank(message = "文件id不可以为空")
    private String fileId;

    @ApiModelProperty(value = "相册id")
    private String albumId;

    @ApiModelProperty(value = "文件备注")
    private String remark;

    @ApiModelProperty(value = "修改人id")
    private Integer userId;

    @ApiModelProperty(value = "是否为封面 0：否 1：是")
    private Integer isCover;
}
