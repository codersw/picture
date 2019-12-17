package com.mango.photoalbum.model;

import com.alicloud.openservices.tablestore.model.DefinedColumnType;
import com.mango.photoalbum.annotation.OTSClass;
import com.mango.photoalbum.annotation.OTSColumn;
import com.mango.photoalbum.annotation.OTSPrimaryKey;
import lombok.*;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
@OTSClass(name = "upload_file_face", searchIndex = false)
public class UploadFileFace {

    /**
     * 文件id
     */
    @OTSPrimaryKey
    private String fileId;

    /**
     * 照片
     */
    @OTSColumn
    private String image;

    /**
     * 识别到的人
     */
    @OTSPrimaryKey
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

}
