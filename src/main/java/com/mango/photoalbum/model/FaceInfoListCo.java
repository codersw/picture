package com.mango.photoalbum.model;

import com.mango.photoalbum.enums.OrderEnum;
import com.mango.photoalbum.enums.PageEnum;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@ApiModel("人脸信息")
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FaceInfoListCo {

    @ApiModelProperty(value = "用户id")
    private Integer userId;

    @ApiModelProperty(value = "文件id")
    private String fileId;

    @ApiModelProperty(value = "当前页")
    private Integer pageIndex = PageEnum.PAGEINDEX.getValue();

    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = PageEnum.PAGESIZE.getValue();

    @ApiModelProperty(value = "时间排序 0 升 1 降 ")
    private Integer order = OrderEnum.DESC.getValue();
}
