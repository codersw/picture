package com.mango.photoalbum.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
@ApiModel("相册")
public class PhotoAlbumListCo {

    /**
     * 关键字
     */
    @ApiModelProperty(value = "关键字")
    private String keyword;

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
