package com.mango.photoalbum.model;

import lombok.*;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class FaceInfo {

    private String Group;

    private String Person;

    private String Image;

    private String ImageUrl;

    private String Content;
}
