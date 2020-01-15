package com.mango.photoalbum.model;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.Date;

@Data
@ApiModel("相册")
public class PhotoAlbumV1Co {

    @ApiModelProperty(value = "相册id,新建可以不传")
    private String albumId;

    @ApiModelProperty(value = "相册标题", required = true)
    @NotBlank(message = "相册标题不能为空")
    private String title;

    @ApiModelProperty(value = "创建人id", required = true)
    @NotNull(message = "创建人id不能为空")
    private Integer userId;

    @ApiModelProperty(value = "拍摄时间格式yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @JsonFormat(pattern = "yyyy-MM-dd",timezone="GMT+8")
    private Date shootTime;

    @ApiModelProperty(value = "拍摄地点")
    private String shootLocation;

    @ApiModelProperty(value = "是否公开")
    private Integer isPublic;

    @ApiModelProperty(value = "部门id")
    private Integer orgId;

    @ApiModelProperty(value = "所有部门id")
    private String orgIdAll;
}
