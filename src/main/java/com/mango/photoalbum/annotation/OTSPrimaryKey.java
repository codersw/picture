package com.mango.photoalbum.annotation;

import com.alicloud.openservices.tablestore.model.PrimaryKeyType;
import com.mango.photoalbum.enums.IndexTypeEnum;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 用于注解在表格存储数据类的头部，传入主键的标识
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OTSPrimaryKey {

    /**
     * 主键字段名字 默认取字段名
     * @return
     */
    String name() default "";

    /**
     * 主键类型 默认字符串
     * @return
     */
    PrimaryKeyType primaryKeyType() default PrimaryKeyType.STRING;

    /**
     * 是否为自增主键 默认不开启必须为INTEGER才可以开启
     * @return
     */
    boolean primaryKeyAuto() default false;

    /**
     * 主键索引只支持 txt keyword integer 默认空
     * @return
     */
    IndexTypeEnum indexType() default IndexTypeEnum.NULL;
}
