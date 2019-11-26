package com.mango.photoalbum.model;


import com.fasterxml.jackson.annotation.JsonFormat;
import com.mango.photoalbum.annotation.OTSClass;
import com.mango.photoalbum.annotation.OTSColumn;
import com.mango.photoalbum.annotation.OTSPrimaryKey;
import lombok.*;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 文件实体
 */
@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
@OTSClass(name = "upload_file")
public class UploadFile {

    /**
     * 文件id
     */
    @OTSPrimaryKey
    private String fileId;

    /**
     * 文件类型
     */
    @OTSColumn
    private String fileType;

    /**
     * 文件路径
     */
    @OTSColumn
    private String filePath;

    /**
     * 备注
     */
    @OTSColumn
    private String remark;

    /**
     * 文件大小
     */
    @OTSColumn
    private Long fileSize;

    /**
     * 文件名字
     */
    @OTSColumn
    private String fileName;

    /**
     * 相册id
     */
    @OTSColumn
    private String albumId;

    /**
     * 上传日期
     */
    @OTSColumn
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone="GMT+8")
    private Date createTime;

    /**
     * 修改时间
     */
    @OTSColumn
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone="GMT+8")
    private Date modifyTime;

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

}
