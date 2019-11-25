package com.mango.photoalbum.model.pojo;

import lombok.*;

import java.util.Date;

/**
 * 相册实体
 */
@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class PhotoAlbum {

    /**
     * 主键相册id
     */
    private String albumId;

    /**
     * 标题
     */
    private String title;

    /**
     * 封面 图片id
     */
    private String cover;

    /**
     * 上传人id
     */
    private Integer userId;

    /**
     * 是否删除
     */
    private Integer isDel;

    /**
     * 上传日期
     */
    private Date createTime;

    /**
     * 修改时间
     */
    private Date modifyTime;

    /**
     * 拍摄时间
     */
    private Date shootTime;

    /**
     * 拍摄地点
     */
    private String shootLocation;
}
