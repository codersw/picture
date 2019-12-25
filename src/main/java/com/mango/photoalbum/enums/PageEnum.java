package com.mango.photoalbum.enums;

/**
 * 分页参数
 * @author swen
 */
public enum PageEnum implements BaseEnum {

    PAGEINDEX(1,"默认页数"),
    PAGESIZE(20,"默认条数");

    private Integer value;
    private String name;

    PageEnum(Integer value, String name){
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
