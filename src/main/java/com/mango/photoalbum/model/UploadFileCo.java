package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;
import javax.validation.constraints.NotNull;

@Data
@ApiModel("上传文件")
public class UploadFileCo {

    @ApiModelProperty(value = "文件id,不传随机生成")
    private String fileId;

    @ApiModelProperty(value = "相册id")
    private String albumId;

    @ApiModelProperty(value = "文件备注")
    private String remark;

    @ApiModelProperty(value = "上传人id")
    private Integer userId;

    @ApiModelProperty(value = "文件")
    @NotNull(message = "文件不能为空")
    private MultipartFile file;

    @ApiModelProperty(value = "是否为封面 0：否 1：是", required = true)
    @NotNull(message = "是否为封面不能为空")
    private Integer isCover = 0;
}
