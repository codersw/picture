package com.mango.photoalbum.model.ots;

import com.alicloud.openservices.tablestore.model.DefinedColumnSchema;
import com.alicloud.openservices.tablestore.model.PrimaryKeySchema;
import lombok.*;
import java.util.List;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TableStore {

    private String tableName;

    private List<PrimaryKeySchema> primaryKeySchemas;

    private List<DefinedColumnSchema> definedColumnSchemas;

}
