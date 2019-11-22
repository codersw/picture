package com.mango.picture.enums;

/**
 * 枚举基础接口
 * @param <T>
 * @author swen
 */
public interface BaseEnum<T> {

    T getValue();

    String getName();
}
