package com.mango.photoalbum.enums;

/**
 * 是否是封面
 * @author swen
 */
public enum IsCoverEnum implements BaseEnum {

    FALSE(0,"否"),
    TRUE(1,"是");

    private Integer value;
    private String name;

    IsCoverEnum(Integer value, String name){
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
