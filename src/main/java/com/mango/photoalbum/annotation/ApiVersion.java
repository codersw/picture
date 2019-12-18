package com.mango.photoalbum.annotation;

import com.mango.photoalbum.constant.ApiVersionConstant;

import java.lang.annotation.*;

/**
 * 用于标记版本号
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ApiVersion {

    /**
     * 接口版本号(对应swagger中的group) 默认v1
     * @return String[]
     */
    String[] value() default ApiVersionConstant.V1;
}
