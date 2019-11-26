package com.mango.photoalbum.model.pojo;

import com.mango.photoalbum.annotation.OTSClass;
import com.mango.photoalbum.annotation.OTSColumn;
import com.mango.photoalbum.annotation.OTSPrimaryKey;
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
@OTSClass(name = "photo_album")
public class PhotoAlbum {

    /**
     * 主键相册id
     */
    @OTSPrimaryKey
    private String albumId;

    /**
     * 标题
     */
    @OTSColumn
    private String title;

    /**
     * 封面 图片id
     */
    @OTSColumn
    private String cover;

    /**
     * 上传人id
     */
    @OTSColumn
    private Integer userId;

    /**
     * 是否删除
     */
    @OTSColumn
    private Integer isDel;

    /**
     * 上传日期
     */
    @OTSColumn
    private Date createTime;

    /**
     * 修改时间
     */
    @OTSColumn
    private Date modifyTime;

    /**
     * 拍摄时间
     */
    @OTSColumn
    private Date shootTime;

    /**
     * 拍摄地点
     */
    @OTSColumn
    private String shootLocation;

}
