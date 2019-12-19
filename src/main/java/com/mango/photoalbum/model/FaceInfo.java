package com.mango.photoalbum.model;


import com.alicloud.openservices.tablestore.model.DefinedColumnType;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.mango.photoalbum.annotation.OTSClass;
import com.mango.photoalbum.annotation.OTSColumn;
import com.mango.photoalbum.annotation.OTSPrimaryKey;
import lombok.*;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
@OTSClass(name = "face_info")
public class FaceInfo {

    /**
     * 主键
     */
    @OTSPrimaryKey
    private String faceId;

    /**
     * 人脸的图片信息
     */
    @OTSColumn
    private String image;

    /**
     * 人脸的人信息
     */
    @OTSColumn
    private String person;

    /**
     * 上传后的文件id
     */
    @OTSColumn
    private String fileId;

    /**
     * 人脸的分组
     */
    @OTSColumn
    private String group;

    /**
     * 人脸的url
     */
    @OTSColumn
    private String imageUrl;

    /**
     * 人脸的base64
     */
    @OTSColumn
    private String content;

    /**
     * 创建人
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    private Integer createUserId;

    /**
     * 最后修改人
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    private Integer modifyUserId;

    /**
     * 是否删除
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    private Integer isDel;

    /**
     * 创建时间
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone="GMT+8")
    private Date createTime;

    /**
     * 最后修改时间
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone="GMT+8")
    private Date modifyTime;
}
