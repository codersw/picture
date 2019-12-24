package com.mango.photoalbum.annotation;


import com.alicloud.openservices.tablestore.model.DefinedColumnType;
import com.mango.photoalbum.enums.AnalyzerEnum;
import com.mango.photoalbum.enums.IndexTypeEnum;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 用于注解在表格存储数据类的头部，传入列的标识
 * @author swen
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OTSColumn {

    /**
     * 列字段名 默认取字段名
     * @return
     */
    String name() default "";

    /**
     * 字段类型 默认字符串
     * @return
     */
    DefinedColumnType definedColumnType() default DefinedColumnType.STRING;

    /**
     * 索引类型 默认不开启
     * @return
     */
    IndexTypeEnum indexType() default IndexTypeEnum.NULL;

    /**
     * 分词类型 默认不开启
     * @return
     */
    AnalyzerEnum analyzer() default AnalyzerEnum.NULL;

    /**
     * 分隔符 默认空
     * @return
     */
    String splitAnalyzerDelimiter() default "";

    /**
     * 最小字符切分单元（minChars）默认0
     * @return
     */
    int fuzzyAnalyzerMinChars() default 0;

    /**
     * 最大字符切分单元（maxChars） 默认0
     * @return
     */
    int fuzzyAnalyzerMaxChars() default 0;

    /**
     * 所有英文字母会转为小写 默认是false
     * @return
     */
    boolean singleWordAnalyzerCaseSensitive() default false;

    /**
     * 代表是否分割英文和数字 默认是false
     * @return
     */
    boolean singleWordAnalyzerDelimitWord() default false;
}
