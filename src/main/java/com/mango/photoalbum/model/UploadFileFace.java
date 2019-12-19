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

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
@OTSClass(name = "upload_file_face")
public class UploadFileFace {

    /**
     * 照片
     */
    @OTSPrimaryKey
    private String image;


    /**
     * 文件id
     */
    @OTSPrimaryKey(indexType = IndexTypeEnum.KEYWORD)
    private String fileId;

    /**
     * 识别到的人
     */
    @OTSColumn(indexType = IndexTypeEnum.KEYWORD)
    private String person;

    /**
     * 匹配分数
     */
    @OTSColumn(definedColumnType = DefinedColumnType.DOUBLE)
    private Double score;

    /**
     * 该人在照片内的坐标
     */
    @OTSColumn
    private String rect;

    /**
     * 添加时间
     */
    @OTSColumn(definedColumnType = DefinedColumnType.INTEGER, indexType = IndexTypeEnum.LONG)
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone="GMT+8")
    private Date createTime;

}
