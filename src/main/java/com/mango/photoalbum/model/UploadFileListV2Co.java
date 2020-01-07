package com.mango.photoalbum.model;

import com.mango.photoalbum.enums.OrderEnum;
import com.mango.photoalbum.enums.PageEnum;
import com.mango.photoalbum.enums.TypeEnum;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@ApiModel("文件列表")
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UploadFileListV2Co {

    @ApiModelProperty(value = "相册id")
    private String albumId;

    @ApiModelProperty(value = "当前页")
    private Integer pageIndex = PageEnum.PAGEINDEX.getValue();

    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = PageEnum.PAGESIZE.getValue();

    @ApiModelProperty(value = "用户id")
    private Integer userId;

    @ApiModelProperty(value = "查询类型")
    private Integer type = TypeEnum.MY.getValue();

    @ApiModelProperty(value = "时间排序 0 升 1 降 ")
    private Integer order = OrderEnum.DESC.getValue();

    @ApiModelProperty(value = "部门id")
    private Integer orgId;

    @ApiModelProperty(hidden = true)
    private Integer total = 0;

}
