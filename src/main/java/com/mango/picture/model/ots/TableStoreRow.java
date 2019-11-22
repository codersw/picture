package com.mango.picture.model.ots;

import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.PrimaryKeyValue;
import lombok.*;

import java.util.Map;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class TableStoreRow extends TableStore {

    private Map<String, PrimaryKeyValue> primaryKeyValue;

    private Map<String, ColumnValue> columnValue;
}
