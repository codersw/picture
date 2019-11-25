package com.mango.photoalbum.model.co;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.Date;

@Data
@ApiModel("相册")
public class PhotoAlbumCo {

    @ApiModelProperty(value = "相册标题", required = true)
    @NotBlank(message = "相册标题不能为空")
    private String title;

    @ApiModelProperty(value = "创建人id", required = true)
    @NotNull(message = "创建人id不能为空")
    private Integer userId;

    @ApiModelProperty(value = "封面")
    private String cover;

    @ApiModelProperty(value = "拍摄时间")
    private Date shootTime;

    @ApiModelProperty(value = "拍摄地点")
    private String shootLocation;
}
