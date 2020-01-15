package com.mango.photoalbum.model;

import com.mango.photoalbum.enums.PageEnum;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
@ApiModel("相册")
public class PhotoAlbumListV1Co {

    @ApiModelProperty(value = "关键字")
    private String keyword = "";

    @ApiModelProperty(value = "当前页")
    private Integer pageIndex = PageEnum.PAGEINDEX.getValue();

    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = PageEnum.PAGESIZE.getValue();

    @ApiModelProperty(value = "部门id")
    private String orgId;
}
