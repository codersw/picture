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
public class UploadFileListV1Co {

    @ApiModelProperty(value = "相册id")
    private String albumId;

    /**
     * 当前页
     */
    @ApiModelProperty(value = "当前页")
    private Integer pageIndex = PageEnum.PAGEINDEX.getValue();

    /**
     * 每页总条数
     */
    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = PageEnum.PAGESIZE.getValue();

    /**
     * 用户id
     */
    @ApiModelProperty(value = "用户id", required = true)
    private Integer userId;

    /**
     * 查询类型
     */
    @ApiModelProperty(value = "查询类型")
    private Integer type = TypeEnum.MY.getValue();

    /**
     * 时间排序
     */
    @ApiModelProperty(value = "时间排序")
    private Integer order = OrderEnum.DESC.getValue();

    /**
     * 总条数
     */
    @ApiModelProperty(hidden = true)
    private Integer total;

}
