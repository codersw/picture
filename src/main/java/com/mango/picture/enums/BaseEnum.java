package com.mango.picture.enums;

/**
 * 枚举基础接口
 * @param <T>
 */
public interface BaseEnum<T> {

    T getValue();

    String getName();
}
