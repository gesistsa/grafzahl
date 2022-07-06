import torch
from simpletransformers.classification import ClassificationModel, ClassificationArgs
import os
os.environ["TOKENIZERS_PARALLELISM"] = "false"

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
            "use_multiprocessing": False,
            "save_steps": -1,
            "save_eval_checkpoints": False,
            "save_model_every_epoch": False,
            "use_early_stopping": True,
            "early_stopping_delta": 0.01,
            "early_stopping_metric": "mcc",
            "early_stopping_metric_minimize": False,
            "early_stopping_patience": 2,
            "num_train_epochs": num_train_epochs},
        use_cuda = py_detect_cuda())
    model.train_model(data)

def py_predict(to_predict, model_type, output_dir, return_raw = False):
    if len(to_predict) == 1:
        to_predict = [to_predict]
    model = ClassificationModel(model_type, output_dir, args = {
        'reprocess_input_data': True,
        "use_multiprocessing": False,
        "fp16": True,
        "use_multiprocessing_for_evaluation": False
    })
    predictions, raw_outputs = model.predict(to_predict)
    if return_raw:
        return(raw_outputs)
    else:
        return(predictions)
