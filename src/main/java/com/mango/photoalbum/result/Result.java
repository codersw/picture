package com.mango.photoalbum.result;

import com.alibaba.fastjson.JSON;
import com.mango.photoalbum.enums.ResultCodeEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import java.io.Serializable;

/**
 * 统一返回类
 * @Author swen
 */
@Data
public class Result<T> implements Serializable {
    
    /**
     *  状态枚举
     **/
    @ApiModelProperty(value = "状态")
    private ResultCodeEnum code;

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
        return JSON.toJSONString(this);
    }

    /**
     * 覆盖状态码返回值
     * @return
     */
    public Integer getCode() {
        return code.getValue();
    }

}
