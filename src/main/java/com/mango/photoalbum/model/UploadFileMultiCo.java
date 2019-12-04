package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@ApiModel("上传多文件")
public class UploadFileMultiCo {

    @ApiModelProperty(value = "上传人id")
    @NotNull(message = "上传人id不能为空")
    private Integer userId;

    @ApiModelProperty(value = "相册id", required = true)
    @NotBlank(message = "相册id不能为空")
    private String albumId;

    @ApiModelProperty("多文件")
    @NotNull(message = "文件不能为空")
    private List<UploadFileCo> uploadFileCos;
}
