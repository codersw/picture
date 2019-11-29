package com.mango.photoalbum.annotation;

import com.alicloud.openservices.tablestore.model.PrimaryKeyOption;
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

    String name() default "";

    PrimaryKeyType primaryKeyType() default PrimaryKeyType.STRING;

    boolean primaryKeyAuto() default false;

    IndexTypeEnum indexType() default IndexTypeEnum.NULL;
}
