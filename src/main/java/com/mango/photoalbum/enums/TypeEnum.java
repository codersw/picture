package com.mango.photoalbum.enums;

/**
 * 查询类型枚举
 * @author swen
 */
public enum TypeEnum implements BaseEnum {

    MY(1,"我的相册");

    private Integer value;
    private String name;

    TypeEnum(Integer value, String name) {
        this.value = value;
        this.name = name;
    }

    @Override
    public Integer getValue() {
        return value;
    }

    @Override
    public String getName() {
        return name;
    }
}
