package com.mango.photoalbum.model.pojo;


import lombok.*;
import java.util.Date;

/**
 * 文件实体
 */
@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class UploadFile {

    /**
     * 文件id
     */
    private String fileId;

    /**
     * 文件类型
     */
    private String fileType;

    /**
     * 文件路径
     */
    private String filePath;

    /**
     * 备注
     */
    private String remark;

    /**
     * 文件大小
     */
    private Long fileSize;

    /**
     * 文件名字
     */
    private String fileName;

    /**
     * 相册id
     */
    private String albumId;

    /**
     * 上传日期
     */
    private Date createTime;

    /**
     * 修改时间
     */
    private Date modifyTime;

    /**
     * 上传人id
     */
    private Integer userId;

    /**
     * 是否删除
     */
    private Integer isDel;

}
