import torch
from simpletransformers.classification import ClassificationModel, ClassificationArgs

def py_detect_cuda():
    return(torch.cuda.is_available())

def py_train(data, num_labels, output_dir, best_model_dir, cache_dir, model_type, model_name, num_train_epochs):
    model = ClassificationModel(
        model_type, model_name, args={
            'reprocess_input_data': True, 
            'overwrite_output_dir': True,
            'fp16': True,
            'output_dir': output_dir,
            "best_model_dir": best_model_dir,
            "cache_dir": cache_dir,
            "use_early_stopping": True,
            "early_stopping_delta": 0.01,
            "early_stopping_metric": "mcc",
            "early_stopping_metric_minimize": False,
            "early_stopping_patience": 2,
            "num_train_epochs": num_train_epochs},
        use_cuda = py_detect_cuda())
    model.train_model(data)
