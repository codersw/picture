package com.mango.photoalbum.model;


import com.alicloud.openservices.tablestore.model.DefinedColumnType;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.mango.photoalbum.annotation.OTSClass;
import com.mango.photoalbum.annotation.OTSColumn;
import com.mango.photoalbum.annotation.OTSPrimaryKey;
import com.mango.photoalbum.enums.AnalyzerEnum;
import com.mango.photoalbum.enums.IndexTypeEnum;
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
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    private Integer fileSize;

    /**
     * 文件名字
     */
    @OTSColumn
    private String fileName;

    /**
     * 相册id
     */
    @OTSColumn(indexType = IndexTypeEnum.KEYWORD)
    private String albumId;

    /**
     * 上传日期
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER, indexType = IndexTypeEnum.LONG)
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone="GMT+8")
    private Date createTime;

    /**
     * 修改时间
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone="GMT+8")
    private Date modifyTime;

    /**
     * 上传人id
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
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
     * 宽
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    private Integer width;

    /**
     * 高
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER)
    private Integer height;

    /**
     * 照片内包扣的人
     */
    @OTSColumn(indexType = IndexTypeEnum.TEXT, analyzer = AnalyzerEnum.Split, splitAnalyzerDelimiter = ",")
    private String persons;

    /**
     * 是否是封面
     */
    @JsonIgnore
    private Integer IsCover;

}
