function gatos()
startup;

img_path = '/Users/Alfonso/Documents/MCC/Aprendizaje/Proyecto/train/cat.' % Prefijo de las imagenes a transformar

for idx=0:12499,
  load('VOC2010/cat_final');  
  model.vis = @() visualizemodel(model, ...
                    1:2:length(model.rules{model.start}));
  image = [img_path int2str(idx) '.jpg'];
  fprintf('Abriendo imagen %s\n', image);
  test(image, model, -1, idx);
end

function test(imname, model, thresh, idx)

target_path = '/Users/Alfonso/Documents/MCC/Aprendizaje/Proyecto/cat_train/cat.'; % Ruta a las imagenes transformadas
target = [target_path int2str(idx) '.jpg'];

cls = model.class;

% load and display image
im = imread(imname);
clf;

% load and display model
model.vis();

% detect objects
[ds, bs] = imgdetect(im, model, thresh);

% prevenir detecciones vacias
if (isempty(ds) | isempty(bs))
  retries = 1;
  delta = -0.1;
  while(retries <= 10 & (isempty(ds) | isempty(bs)))
    thresh = thresh - delta;
    retries = retries + 1;
    fprintf('Reintento %i con threshold %f\n', retries, thresh);
    [ds, bs] = imgdetect(im, model, thresh);
  end
end

if (isempty(ds) | isempty(bs))
    fprintf('no se pudo :( saliendo...\n');
      clf;
    image(im);
    export_fig([ target ]);
    return;
end

top = nms(ds, 0.5);
clf;
if model.type == model_types.Grammar
  bs = [ds(:,1:4) bs];
end

if model.type == model_types.MixStar
  % get bounding boxes
  bbox = bboxpred_get(model.bboxpred, ds, reduceboxes(model, bs));
  bbox = clipboxes(im, bbox);
  top = nms(bbox, 0.5);
  clf;

  max_rect = max_box(bbox(top, :));
  cropped = imcrop(im, max_rect(1:4));
  image(cropped);
  export_fig([ target ]);

  % disp('press any key to continue'); pause;
end

fprintf('\n');
