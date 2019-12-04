package com.mango.photoalbum.model;


import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 统一返回类
 * @Author swen
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ApiModel
public class Result<T> implements Serializable {
    
    /**
     *  状态枚举
     **/
    @ApiModelProperty(value = "状态")
    private Integer code;

    /**
     *  消息
     **/
    @ApiModelProperty(value = "消息")
    private String message;

    /**
     *  内容
     **/
    @ApiModelProperty(value = "内容")
    private T data;

    /**
     * 返回json
     * @return
     */
    @Override
    public String toString() {
        return JSONObject.toJSONString(this, SerializerFeature.IgnoreNonFieldGetter);
    }

}
