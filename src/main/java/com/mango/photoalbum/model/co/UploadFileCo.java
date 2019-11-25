package com.mango.photoalbum.model.co;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;
import javax.validation.constraints.NotNull;

@Data
@ApiModel("上传文件")
public class UploadFileCo {

    @ApiModelProperty(value = "文件id")
    private String fileId;

    @ApiModelProperty(value = "文件备注")
    private String remark;

    @ApiModelProperty(value = "上传人id")
    @NotNull(message = "上传人id不能为空")
    private Integer userId;

    @ApiModelProperty(value = "文件")
    @NotNull(message = "文件不能为空")
    private MultipartFile file;
}
