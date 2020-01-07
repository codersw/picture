package com.mango.photoalbum.model;

import com.mango.photoalbum.enums.PageEnum;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

@Data
@ApiModel("文件列表")
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UploadFileListCo {

    @ApiModelProperty(value = "相册id", required = true)
    @NotBlank(message = "相册id不可以为空")
    private String albumId;

    @ApiModelProperty(value = "当前页")
    private Integer pageIndex = PageEnum.PAGEINDEX.getValue();

    @ApiModelProperty(value = "每页总条数")
    private Integer pageSize = PageEnum.PAGESIZE.getValue();

    @ApiModelProperty(value = "用户id")
    private Integer userId;

    @ApiModelProperty(hidden = true)
    private Integer total = 0;

}
