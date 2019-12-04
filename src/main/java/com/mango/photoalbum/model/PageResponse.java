package com.mango.photoalbum.model;


import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ApiModel
public class PageResponse<T> {

    /**
     * 总条数
     **/
    @ApiModelProperty(value = "总条数")
    private Integer total;

    /**
     * 内容集合
     **/
    @ApiModelProperty(value = "内容集合")
    private List<T> list;

    /**
     * 多余数据
     */
    @ApiModelProperty(value = "多余数据")
    private Object data;

    @Override
    public String toString() {
        return JSONObject.toJSONString(this, SerializerFeature.IgnoreNonFieldGetter);
    }
}

