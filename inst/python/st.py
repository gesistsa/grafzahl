import torch
from simpletransformers.classification import ClassificationModel, ClassificationArgs

def py_detect_cuda():
    return(torch.cuda.is_available())

def py_train(data, num_labels):
    model = ClassificationModel(
        "xlmroberta", "xlm-roberta-base", args={'learning_rate':1e-5, 
                                                'num_train_epochs': 4, 
                                                'reprocess_input_data': True, 
                                                'overwrite_output_dir': True,
                                                'fp16': False},
        use_cuda = py_detect_cuda())
    model.train_model(data)
