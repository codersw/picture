package com.mango.photoalbum.enums;

import com.alicloud.openservices.tablestore.model.search.sort.SortOrder;

/**
 * 排序参数
 * @author swen
 */
public enum OrderEnum implements BaseEnum {

    ASC(0, SortOrder.ASC),
    DESC(1, SortOrder.DESC);

    private Integer value;
    private SortOrder name;

    OrderEnum(Integer value, SortOrder name){
        this.value = value;
        this.name = name;
    }

    @Override
    public Integer getValue() {
        return value;
    }

    @Override
    public SortOrder getName() {
        return name;
    }
}
