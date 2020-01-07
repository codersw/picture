package com.mango.photoalbum.enums;

/**
 * 是否公开枚举
 * @author swen
 */
public enum IsPublicEnum implements BaseEnum {

    PUBLIC(0,"公开"),
    NOPUBLIC(1,"不公开");

    private Integer value;
    private String name;

    IsPublicEnum(Integer value, String name){
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
