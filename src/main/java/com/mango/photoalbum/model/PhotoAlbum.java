package com.mango.photoalbum.model;

import com.alicloud.openservices.tablestore.model.DefinedColumnType;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.mango.photoalbum.annotation.OTSClass;
import com.mango.photoalbum.annotation.OTSColumn;
import com.mango.photoalbum.annotation.OTSPrimaryKey;
import com.mango.photoalbum.enums.IndexTypeEnum;
import lombok.*;
import org.springframework.format.annotation.DateTimeFormat;
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
    @OTSColumn(indexType = IndexTypeEnum.TEXT)
    private String title;

    /**
     * 封面 图片id
     */
    @OTSColumn
    private String cover;

    /**
     * 上传人id
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER, indexType = IndexTypeEnum.LONG)
    private Integer createUserId;

    /**
     * 修改人id
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    private Integer modifyUserId;

    /**
     * 是否删除
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER, indexType = IndexTypeEnum.LONG)
    private Integer isDel;

    /**
     * 上传日期
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone="GMT+8")
    private Date createTime;

    /**
     * 修改时间
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER, indexType = IndexTypeEnum.LONG)
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone="GMT+8")
    private Date modifyTime;

    /**
     * 拍摄时间
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @JsonFormat(pattern = "yyyy-MM-dd", timezone="GMT+8")
    private Date shootTime;

    /**
     * 拍摄地点
     */
    @OTSColumn(indexType = IndexTypeEnum.TEXT)
    private String shootLocation;

    /**
     * 是否公开
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER, indexType = IndexTypeEnum.LONG)
    private Integer isPublic;

    /**
     * 部门id
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER, indexType = IndexTypeEnum.LONG)
    private Integer orgId;

    /**
     * 封面 图片url
     */
    private String coverPath;

}
