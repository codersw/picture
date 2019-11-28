package com.mango.photoalbum.model;


import lombok.*;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class PageResponse<T> {

    /**
     * 总条数
     **/
    private Integer total;

    /**
     * 内容集合
     **/
    private List<T> list;

    /**
     * 多余数据
     */
    private Object data;
}

