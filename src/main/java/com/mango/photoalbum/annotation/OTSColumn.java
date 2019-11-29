package com.mango.photoalbum.annotation;


import com.alicloud.openservices.tablestore.model.DefinedColumnType;
import com.mango.photoalbum.enums.IndexTypeEnum;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 用于注解在表格存储数据类的头部，传入列的标识
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OTSColumn {

    String name() default "";

    DefinedColumnType definedColumnType() default DefinedColumnType.STRING;

    IndexTypeEnum indexType() default IndexTypeEnum.NULL;
}
