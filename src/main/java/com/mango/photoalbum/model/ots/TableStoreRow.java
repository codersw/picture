package com.mango.photoalbum.model.ots;

import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.PrimaryKeyValue;
import lombok.*;

import java.util.Map;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TableStoreRow {

    private String tableName;

    private Map<String, PrimaryKeyValue> primaryKeyValue;

    private Map<String, ColumnValue> columnValue;
}
