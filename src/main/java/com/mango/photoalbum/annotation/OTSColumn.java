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
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OTSColumn {

    String name() default "";

    DefinedColumnType definedColumnType() default DefinedColumnType.STRING;

    IndexTypeEnum indexType() default IndexTypeEnum.NULL;

    AnalyzerEnum analyzer() default AnalyzerEnum.NULL;

    /**
     * 分隔符
     * @return
     */
    String splitAnalyzerDelimiter() default "";

    /**
     * 最小字符切分单元（minChars）
     * @return
     */
    int fuzzyAnalyzerMinChars() default 0;

    /**
     * 最大字符切分单元（maxChars）
     * @return
     */
    int fuzzyAnalyzerMaxChars() default 0;

    /**
     * 参数caseSensitive 默认是false（所有英文字母会转为小写）
     * @return
     */
    boolean singleWordAnalyzerCaseSensitive() default false;

    /**
     * delimitWord代表是否分割英文和数字
     * @return
     */
    boolean singleWordAnalyzerDelimitWord() default false;
}
