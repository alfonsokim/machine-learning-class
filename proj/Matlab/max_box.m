function max_rect = max_box( boxes )
startup;

% Formato: x1, y1, x2, y2, wtf

num_boxes = size(boxes, 1);
max_area = 0;
max_area_idx = 0;
max_rect = boxes(1, :);

for idx=1:num_boxes,

    % fprintf('\n\ncaja %i\n', idx);

    a_box = boxes(idx, :);
    x1 = a_box(1);
    y1 = a_box(2);
    x2 = a_box(3);
    y2 = a_box(4);

    % fprintf('x1 %f \n', x1);
    % fprintf('x2 %f \n', x2);
    % fprintf('y1 %f \n', y1);
    % fprintf('y2 %f \n', y2);

    area = ((x2-x1) * (y2-y1));
    % fprintf('area %f \n', area);

    if(area > max_area),
        max_rect = a_box;
        max_area = area;
        max_area_idx = idx;
    end

end

% fprintf('max area: %i\n', max_area_idx)

end

