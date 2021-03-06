package com.mango.photoalbum.annotation;


import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 用于注解在表格存储数据类的头部，传入该类的标识
 * @author swen
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface OTSClass {

    /**
     * ots表名 默认取类名
     * @return
     */
    String name() default "";

    /**
     * 是否开启多元索引 默认开启
     * @return
     */
    boolean searchIndex() default true;
}
